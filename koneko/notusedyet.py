# For ui

class AbstractDataWrapper(data.AbstractData):
    def remove_dir_if_exist(self) -> 'Maybe[IO]':
        if self.download_path.is_dir():
            rmtree(self.download_path)

    def dir_not_empty(self) -> bool:
        if self.download_path.is_dir() and (_dir := os.listdir(self.download_path)):
            # Is a valid directory and it's not empty, but data has not been fetched yet
            try:
                self.all_names
            except (KeyError, AttributeError):
                return True

            # Exclude the .koneko file
            if '.koneko' in sorted(_dir)[0]:
                return self._dir_up_to_date(sorted(_dir)[1:])
            return self._dir_up_to_date(_dir)
        return False

    def _dir_up_to_date(self, _dir) -> bool:
        # O(1) time
        if len(_dir) < len(self.all_names):
            return False

        # Should not fail because try-except early returned
        for name, _file in zip(self.all_names, sorted(_dir)):
            if name.replace('/', '') not in _file:
                return False
        return True

class UserDataWrapper(AbstractDataWrapper):
    def read_invis(self) -> 'IO[int]':
        with open(self.download_path / '.koneko', 'r') as f:
            return int(f.read())

    def save_number_of_artists(self) -> 'IO':
        """"Save the number of artists == splitpoint
        So later accesses, which will not request, can display properly
        """
        with open(self.download_path / '.koneko', 'w') as f:
            f.write(str(self.splitpoint))
